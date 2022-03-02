import JSynBackend.Backend;

import java.io.IOException;

public class %name% {
  public static void main (String[] args)
  {
    try {
      new Backend()
        .loadSamples(new String[] %instrument%)
        .play(%bpm%, new int[][] %pattern%, true);
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}