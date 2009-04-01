package edu.brown.cs.squirrel.arview;

import android.content.Context;
import android.view.SurfaceView;
import android.view.SurfaceHolder;
import android.graphics.PixelFormat;
import android.hardware.Camera;
import android.widget.Toast;
import android.os.Handler;

class ARViewView extends SurfaceView {
	final private static long delay = 1000;
	
	final private SurfaceHolder holder;
	final private Toast toast;
	final private Handler handler = new Handler();
	private Camera camera;
	private byte[] preview;
	private Object preview_lock = new Object();
	private Thread thread;
	
	public ARViewView(Context context) {
		super(context);
		
		toast = Toast.makeText(context, "", Toast.LENGTH_SHORT);
		
		holder = getHolder();
		holder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);
		
		holder.addCallback(new SurfaceHolder.Callback() {
			public void surfaceCreated(SurfaceHolder holder) {
				camera = Camera.open();
				camera.setPreviewDisplay(holder);
			}
			
			public void surfaceChanged(SurfaceHolder holder, int format, int w, int h) {
				final Camera.Parameters params = camera.getParameters();
				
				// these either have no effect or we can't change them
				params.setPreviewFormat(PixelFormat.YCbCr_422_SP);
				//params.setPreviewFrameRate(2);
				//params.setPreviewSize(320, 240);
				//params.setPreviewSize(w, h);
				
				camera.setParameters(params);
				
				if (holder.isCreating()) {
					camera.startPreview();
					
					thread = new Thread(new Runnable() {
						final Camera.PreviewCallback cb = new Camera.PreviewCallback() {
							public void onPreviewFrame(byte[] data, Camera camera) {
								synchronized (preview_lock) {
									camera.setPreviewCallback(null);
									preview = data;
									preview_lock.notify();
								}
							}
						};
						
						public void run() {
							while (!Thread.interrupted()) {
								synchronized (preview_lock) {
									camera.setPreviewCallback(cb);
									try { preview_lock.wait(); }
									catch (InterruptedException e) { break; }
									process(preview);
								}
								
								try { Thread.sleep(delay); }
								catch (InterruptedException e) { break; }
							}
							
							camera.setPreviewCallback(null);
						}
					});
					
					thread.start();
				}
			}
			
			public void surfaceDestroyed(SurfaceHolder holder) {
				// should check if this stuff was started
				thread.interrupt();
				thread = null;
				
				camera.setPreviewCallback(null);
				camera.stopPreview();
				camera.release();
				camera = null;
			}
		});
	}
	
	private void debug(final CharSequence cs) {
		handler.post(new Runnable() {
			public void run() {
				toast.setText(cs);
				toast.show();
			}
		});
	}
	
	private void process(byte[] data) {
		if (data != null) {
			
			final long time = System.currentTimeMillis();
			
			final int uv_i = data.length * 2 / 3;
			
			int red = 0, green = 0, blue = 0,
			    red_x = 0, green_x = 0, blue_x = 0,
			    red_y = 0, green_y = 0, blue_y = 0;
			
			for (int x = 0; x < 240; x++)
				for (int y = 0; y < 160; y++) {
					final int i = uv_i + (y * 240 + x) * 2;
					final int u = 0xFF & (int) data[i + 1],
							  v = 0xFF & (int) data[i];
					
					if (u < 112 && v >= 144) {
						red++;
						red_x += x;
						red_y += y;
					}
					if (u < 112 && v < 112) {
						green++;
						green_x += x;
						green_y += y;
					}
					if (u >= 144 && v < 112) {
						blue++;
						blue_x += x;
						blue_y += y;
					}
				}
			
			System.out.println("" + (System.currentTimeMillis() - time));
			debug("Time: " + (System.currentTimeMillis() - time) +
				(red > 0 ? "\nR: " + red + ":" + red_x / red + "," + red_y / red : "") +
				(green > 0 ? "\nG: " + green + ":" + green_x / green + "," + green_y / green : "") +
				(blue > 0 ? "\nB: " + blue + ":" + blue_x / blue + "," + blue_y / blue : ""));
		}
	}
	
	protected void finalize() {
		if (thread != null) {
			thread.interrupt();
			thread = null;
		}
		if (camera != null) {
			// just in case.  who knows.
			camera.setPreviewCallback(null);
			camera.stopPreview();
			camera.release();
			camera = null;
		}
	}
}
