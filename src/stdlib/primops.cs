
public static class primops
{
    // Integer arithmetic
    public static int addi(int x, int y)
    {
        return x + y;
    }
    public static int subi(int x, int y)
    {
        return x - y;
    }
    public static int muli(int x, int y)
    {
        return x * y;
    }
    public static int divi(int x, int y)
    {
        return x / y;
    }
    public static int modi(int x, int y)
    {
        return x % y;
    }

    // Integer bitwise ops
    public static int ori(int x, int y)
    {
        return x | y;
    }
    public static int andi(int x, int y)
    {
        return x & y;
    }
    public static int xori(int x, int y)
    {
        return x ^ y;
    }
    public static int shri(int x, int y)
    {
        return x >> y;
    }
    public static int shli(int x, int y)
    {
        return x << y;
    }

    // Integer comparison
    public static bool eqi(int x, int y)
    {
        return x == y;
    }
    public static bool neqi(int x, int y)
    {
        return x != y;
    }
    public static bool lti(int x, int y)
    {
        return x < y;
    }
    public static bool lei(int x, int y)
    {
        return x <= y;
    }
    public static bool gti(int x, int y)
    {
        return x > y;
    }
    public static bool gei(int x, int y)
    {
        return x >= y;
    }

    // Boolean negation
    public static bool notb(bool flag)
    {
        return !flag;
    }
}
