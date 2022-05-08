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
        .play(%bpm%, new int[][] %pattern%, true);
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
