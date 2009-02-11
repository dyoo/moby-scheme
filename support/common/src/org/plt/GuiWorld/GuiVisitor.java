package org.plt.GuiWorld;

public interface GuiVisitor {
    void visit(Row r);
    void visit(Col c);
    void visit(Message m);
    void visit(BoxGroup b);
    void visit(Canvas c);
    void visit(Button b);
    void visit(Slider s);
    void visit(DropDown d);
    void visit(TextField t);
    void visit(CheckBox c);
}