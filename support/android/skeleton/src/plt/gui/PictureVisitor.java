package plt.gui;


public interface PictureVisitor {
    public void visit(Scene p, int x, int y);
    public void visit(RectanglePicture p, int x, int y);
    public void visit(FilePicture p, int x, int y);
    public void visit(CirclePicture p, int x, int y);
    public void visit(TextPicture p, int x, int y);
}
