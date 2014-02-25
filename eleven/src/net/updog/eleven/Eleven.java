package net.updog.eleven;

import android.app.Application;
import android.util.Log;

import com.parse.Parse;
import com.parse.ParseFacebookUtils;

public class Eleven extends Application {

  @Override
  public void onCreate() {
    super.onCreate();
    Parse.initialize(
        this,
        getResources().getString(R.string.parse_app_id),
        getResources().getString(R.string.parse_client_key));
    ParseFacebookUtils.initialize(getResources().getString(
        R.string.facebook_app_id));
  };
}
