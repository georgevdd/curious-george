package net.updog.eleven;

import java.util.List;

import com.parse.FindCallback;
import com.parse.ParseClassName;
import com.parse.ParseObject;
import com.parse.ParseUser;

@ParseClassName("Round")
public class Round extends ParseObject {
  public List<ParseUser> getPlayers() {
    return getList("Players");
  }
  
  public void addPlayer(ParseUser player) {
    addUnique("Players", player);
  }

  public static void roundsForPlayer(ParseUser player,
      FindCallback<Round> callback) {
  }
}
