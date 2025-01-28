class Hello
{
    int g;
    
    void main()
    {
        int b;
        b = 5;
        print(b,b);
    }
    
    int square( bool x )
    {
        int y;
        y = x*x;
        return y;   
    }

    int abs(int x)
    {
        if (x<0)
            x = 0-x;
        return x;
    }
    
    int fac(int x)
    {
        int r; int t;
        t=1; r=1;
        while (t<=x)
        {
            r = r*t;
            t = t+1;
        }
        return r;
   }
}
