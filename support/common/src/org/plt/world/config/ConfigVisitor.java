package org.plt.world.config;

public interface ConfigVisitor {
    void visit(OnTick c);
    void visit(OnMouse c);
    void visit(OnKey c);
    void visit(OnMessage c);
    void visit(OnLocationChange c);
    void visit(OnTilt c);
    void visit(OnAcceleration c);
    void visit(OnRedraw c);
    void visit(StopWhen c);
}