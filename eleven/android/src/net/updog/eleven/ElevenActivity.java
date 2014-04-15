package net.updog.eleven;

import java.util.Arrays;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.widget.ListView;
import android.widget.TextView;

import com.facebook.Request;
import com.facebook.Response;
import com.facebook.model.GraphUser;
import com.parse.LogInCallback;
import com.parse.ParseException;
import com.parse.ParseFacebookUtils;
import com.parse.ParseQuery;
import com.parse.ParseQueryAdapter;
import com.parse.ParseUser;

public class ElevenActivity extends Activity {
  private static final int PARSE_LOGIN = 0;
  private static final int PICK_FRIENDS = 1;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    logIn();
  }

  private void logIn() {
    ParseFacebookUtils.logIn(Arrays.asList("user_friends"), this, PARSE_LOGIN,
        new FacebookLoginCallback());
  }

  private class FacebookLoginCallback extends LogInCallback {
    @Override
    public void done(final ParseUser parseUser, ParseException err) {
      if (parseUser == null) {
        Log.e("Eleven", "Nobody home.");
        ElevenActivity.this.finish();
        return;
      }

      setContentView(R.layout.activity_eleven);

      Request.newMeRequest(ParseFacebookUtils.getSession(),
          new FacebookMeRequestCallback()).executeAsync();

      ((ListView) findViewById(R.id.round_list))
          .setAdapter(new ParseQueryAdapter<Round>(ElevenActivity.this,
              new OpenRoundsQueryFactory()));
    }
  }

  private class FacebookMeRequestCallback implements Request.GraphUserCallback {
    @Override
    public void onCompleted(GraphUser facebookUser, Response response) {
      TextView welcome = (TextView) findViewById(R.id.hello_world);
      if (facebookUser != null) {
        welcome.setText("Hello, " + facebookUser.getName() + "!");
        ParseUser parseUser = ParseUser.getCurrentUser();
        parseUser.put("fbid", facebookUser.getId());
        parseUser.saveInBackground();
      } else {
        welcome.setText("Hello, mystery visitor ...");
      }
    }
  }

  private final class OpenRoundsQueryFactory implements
      ParseQueryAdapter.QueryFactory<Round> {
    @Override
    public ParseQuery<Round> create() {
      return ParseQuery.getQuery(Round.class).whereEqualTo("Players",
          ParseUser.getCurrentUser());
    }
  }

  public void newRound(View newRoundButton) {
    Intent pickFriends = new Intent(this, PickParseFriendsActivity.class);
    startActivityForResult(pickFriends, PICK_FRIENDS);
    // PickFriendsActivity.populateParameters(pickFriends, null, false, true);
    // startActivityForResult(pickFriends, PICK_FRIENDS);
    //
    // Round round = new Round();
    // round.addPlayer(ParseUser.getCurrentUser());
    // // TODO: Pick opponent(s)
    // round.saveInBackground();
  }

  @Override
  public void onActivityResult(int requestCode, int resultCode, Intent data) {
    switch (requestCode) {
    case PARSE_LOGIN:
      // This only happens if the user wasn't already logged in.
      ParseFacebookUtils.finishAuthentication(requestCode, resultCode, data);
      break;
    case PICK_FRIENDS:
      super.onActivityResult(requestCode, resultCode, data);

      break;
    default:
      super.onActivityResult(requestCode, resultCode, data);
    }
  }

  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
    // Inflate the menu; this adds items to the action bar if it is present.
    getMenuInflater().inflate(R.menu.eleven, menu);
    return true;
  }

}
