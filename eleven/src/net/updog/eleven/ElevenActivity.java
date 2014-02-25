package net.updog.eleven;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.widget.TextView;

import com.facebook.Request;
import com.facebook.Response;
import com.facebook.model.GraphUser;
import com.parse.LogInCallback;
import com.parse.Parse;
import com.parse.ParseException;
import com.parse.ParseFacebookUtils;
import com.parse.ParseUser;

public class ElevenActivity extends Activity {
  private static final int PARSE_LOGIN = 0;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    Log.e("MyApp", "onCreate");
    super.onCreate(savedInstanceState);

    ParseFacebookUtils.logIn(this, PARSE_LOGIN, new LogInCallback() {
      @Override
      public void done(ParseUser user, ParseException err) {
        Log.e("MyApp", "done: " + user + " " + err);
        if (user == null) {
          Log.d("MyApp", "Uh oh. The user cancelled the Facebook login.");
          ElevenActivity.this.finish();
          return;
        }

        setContentView(R.layout.activity_eleven);
        
        if (user.isNew()) {
          Log.d("MyApp", "User signed up and logged in through Facebook!");
        } else {
          Log.d("MyApp", "User logged in through Facebook! Yay!");
        }

        Request.newMeRequest(ParseFacebookUtils.getSession(),
            new Request.GraphUserCallback() {
              @Override
              public void onCompleted(GraphUser user, Response response) {
                TextView welcome = (TextView) findViewById(R.id.hello_world);
                if (user != null) {
                  welcome.setText("Hello, " + user.getName() + "!");
                } else {
                  welcome.setText("Hello, mystery visitor ...");
                }
              }
            }).executeAsync();
      }
    });
  }

  @Override
  public void onActivityResult(int requestCode, int resultCode, Intent data) {
    switch (requestCode) {
    case PARSE_LOGIN:
      // This only happens if the user wasn't already logged in.
      ParseFacebookUtils.finishAuthentication(requestCode, resultCode, data);
      break;
    }
    super.onActivityResult(requestCode, resultCode, data);
  }

  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
    // Inflate the menu; this adds items to the action bar if it is present.
    getMenuInflater().inflate(R.menu.eleven, menu);
    return true;
  }

}
