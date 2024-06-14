package my;


public class HelloWorld
{
    public static void main (String[] args)
    {
        sayHelloTo (args.length > 0 ? args[0]: "world");
    }

    public static void sayHelloTo (String whom)
    {
        System.out.println ("Hello, " + whom + "!");
    }
}
