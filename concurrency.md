## double-checked locking
不用获得锁便可以检测锁的条件, 用来减少第一次判断锁的条件的开销, 经常被用在单例模式中. 但是这是不安全的一种模式.

```
public class Singleton {
    private static Singleton instance = null;
    private Singleton(){}
   
    public static Singleton  getInstance() {
       if(instance == null) {
           instance = new Singleton();
       }
       return instance;
    }
}
```
这段代码在多线程环境中有明显的错误(线程 A, B 同时判断instance == null)

于是有了下面的代码:
```
 public synchronized static Singleton getInstance() {
       if(instance == null) {
           instance = new Singleton();
       }
       return instance;
    }
```
