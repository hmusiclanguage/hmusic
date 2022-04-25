import JSynBackend.Backend;

import java.io.IOException;

import com.jsyn.data.Function;

public class %name% {
  public static void main (String[] args)
  {
    try {
      new Backend()
        .loadSamples(new String[] %instrument%)
          %effect%
          .attachEffect(0, new Function() {
                  public double evaluate (double x)
                  {
                      return 0;
                  }
              })
        .play(%bpm%, new int[][] %pattern%, true);
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
