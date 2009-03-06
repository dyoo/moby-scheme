package org.plt.guiworld;


public class GuiRenderer implements GuiVisitor {

    public GuiRenderer() {
    }


    public void visit(Row r) {

    }
    

    // public void visit(Col c) {
    //     }
    

    public void visit(Message m) {
    }



    // FIXME: implement these!
 
    public void visit(BoxGroup b) {}
    public void visit(Canvas c) {}
    public void visit(Button b) {}
    public void visit(Slider s) {}
    public void visit(DropDown d) {}
    public void visit(TextField t) {}
    public void visit(CheckBox c) {}
}
