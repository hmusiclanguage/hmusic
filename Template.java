import JavaRuntime.Runtime;

import java.io.IOException;

public class %name% {
  public static void main (String[] args)
  {
    try {
      new Runtime()
        .loadSamples(new String[] %instrument%)
        %effect%
        .play(%bpm%, new int[][] %pattern%, true);
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
