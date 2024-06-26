package u04.monads;

import javax.swing.*;
import java.awt.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.*;

class SwingFunctionalFacade {

    public static interface Frame {
        Frame setSize(int width, int height);
        
        // Adders
        Frame addButton(String text, String name);
        Frame addLabel(String text, String name);
        Frame addTextField(String text, String name);
        
        Frame showToLabel(String text, String name);
        Frame show();

        String labelText(String name);
        String texFieldText(String name);
        
        Supplier<String> events();        
    }

    public static Frame createFrame(){
        return new FrameImpl();
    }

    /*
    private static class FrameImpl implements Frame {
      ...
    }
    */

    private static class FrameImpl implements Frame {
        private final JFrame jframe = new JFrame();
        private final Map<String, JButton> buttons = new HashMap<>();
        private final Map<String, JLabel> labels = new HashMap<>();
        private final Map<String, JTextField> textFiels = new HashMap<>();
        private final LinkedBlockingQueue<String> eventQueue = new LinkedBlockingQueue<>();
        private final Supplier<String> events = () -> {
            try{
                return eventQueue.take();
            } catch (InterruptedException e){
                return "";
            }
        };
        public FrameImpl() {
            this.jframe.setLayout(new FlowLayout());
            this.jframe.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        }

        @Override
        public Frame setSize(int width, int height) {
            this.jframe.setSize(width, height);
            return this;
        }

        @Override
        public Frame addButton(String text, String name) {
            JButton jb = new JButton(text);
            jb.setActionCommand(name);
            this.buttons.put(name, jb);
            jb.addActionListener(e -> {
                try {
                    eventQueue.put(name);
                } catch (InterruptedException ex){}
            });
            this.jframe.getContentPane().add(jb);
            return this;
        }

        @Override
        public Frame addLabel(String text, String name) {
            JLabel jl = new JLabel(text);
            this.labels.put(name, jl);
            this.jframe.getContentPane().add(jl);
            return this;
        }

        @Override
        public Frame addTextField(String text, String name) {
            JTextField jtf = new JTextField(text, 5);
            this.textFiels.put(name, jtf);
            this.jframe.getContentPane().add(jtf);
            return this;
        }

        @Override
        public String labelText(String name) {
            return this.labels.get(name).getText();
        }

        @Override
        public String texFieldText(String name) {
            return this.textFiels.get(name).getText();
        }

        @Override
        public Supplier<String> events() {
            return events;
        }

        @Override
        public Frame showToLabel(String text, String name) {
            this.labels.get(name).setText(text);
            return this;
        }

        @Override
        public Frame show() {
            this.jframe.setVisible(true);
            return this;
        }

    }
}
