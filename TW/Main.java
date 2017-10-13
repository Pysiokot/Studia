public class Main {
	public static int Val;
	public final static int n = 2;
	public static Thread[] threads = new Thread[n];
	
	public static void main(String[] args) throws InterruptedException {
		
		for(int i = 0; i < n; i++) {
			threads[i] = new MyThread();
			threads[i].start();
		}
		
		for(int i = 0; i < n; i ++) {
			threads[i].join();
		}
		
		System.out.println(Val);
	}
}
