public class MyThread extends Thread{
	public void run() {
		try {
			this.sleep(10000);
		}
		catch(Exception e) {
			
		}
		for(int i = 0; i < 20; i++) {
			Main.Val++;
		}
	}
}
