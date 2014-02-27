package net.updog.eleven;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.FragmentActivity;
import android.util.Log;
import android.widget.Toast;
import com.facebook.FacebookException;
import com.facebook.Request;
import com.facebook.Response;
import com.facebook.model.GraphUser;
import com.facebook.widget.FriendPickerFragment;
import com.facebook.widget.PickerFragment;
import com.facebook.widget.PickerFragment.GraphObjectFilter;
import com.parse.FindCallback;
import com.parse.ParseException;
import com.parse.ParseFacebookUtils;
import com.parse.ParseQuery;
import com.parse.ParseUser;

public class PickParseFriendsActivity extends FragmentActivity {
  FriendPickerFragment friendPickerFragment;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.pick_parse_friends_activity);

    friendPickerFragment = (FriendPickerFragment) getSupportFragmentManager()
        .findFragmentById(R.id.friend_picker_fragment);
    if (savedInstanceState == null) {
      // If this is the first time we have created the fragment, update its
      // properties based on
      // any parameters we received via our Intent.
      friendPickerFragment.setSettingsFromBundle(getIntent().getExtras());
    }

    friendPickerFragment
        .setOnErrorListener(new PickerFragment.OnErrorListener() {
          @Override
          public void onError(PickerFragment<?> fragment,
              FacebookException error) {
            PickParseFriendsActivity.this.onError(error);
          }
        });

    // We finish the activity when either the Done button is pressed or when a
    // friend is selected (since only a single friend can be selected).
    friendPickerFragment
        .setOnSelectionChangedListener(new PickerFragment.OnSelectionChangedListener() {
          @Override
          public void onSelectionChanged(PickerFragment<?> fragment) {
            if (friendPickerFragment.getSelection() != null) {
              finishActivity();
            }
          }
        });

    friendPickerFragment
        .setOnDoneButtonClickedListener(new PickerFragment.OnDoneButtonClickedListener() {
          @Override
          public void onDoneButtonClicked(PickerFragment<?> fragment) {
            finishActivity();
          }
        });

    Log.v("PickPlayerActivity", "Getting Facebook friends ...");
    Request.newMyFriendsRequest(ParseFacebookUtils.getSession(),
        new FacebookFriendsCallback()).executeAsync();
  }

  private final class FacebookFriendsCallback implements
      Request.GraphUserListCallback {
    @Override
    public void onCompleted(List<GraphUser> users, Response response) {
      Log.v("PickPlayerActivity", "Got Facebook friends.");

      Collection<String> friendFbids = new ArrayList<String>();
      for (GraphUser user : users) {
        friendFbids.add(user.getId());
      }

      Log.v("PickPlayerActivity",
          "Getting Parse users who are Facebook friends ...");
      new ParseQuery<ParseUser>(ParseUser.class).whereContainedIn("fbid",
          friendFbids).findInBackground(new ParseFriendsCallback());
    }
  }

  private final class ParseFriendsCallback extends FindCallback<ParseUser> {
    @Override
    public void done(List<ParseUser> users, ParseException e) {
      Log.v("PickPlayerActivity",
          "Got Parse users who are Facebook friends ...");

      final Set<String> parseFriendFbids = new HashSet<String>();
      for (ParseUser user : users) {
        parseFriendFbids.add(user.getString("fbid"));
      }

      friendPickerFragment.setFilter(new FbidFilter(parseFriendFbids));
      friendPickerFragment.loadData(false);
    }
  }

  private final class FbidFilter implements GraphObjectFilter<GraphUser> {
    private final Set<String> fbids;

    public FbidFilter(Set<String> fbids) {
      this.fbids = fbids;
    }

    @Override
    public boolean includeItem(GraphUser graphObject) {
      return fbids.contains(graphObject.getId());
    }
  }

  private void finishActivity() {
    List<GraphUser> selection = friendPickerFragment.getSelection();
    Intent result = new Intent();
    result.putExtra("fbid", selection.iterator().next().getId());
    setResult(RESULT_OK, result);
    finish();
  }

  private void onError(Exception error) {
    String text = getString(R.string.load_friends_failed);
    Toast toast = Toast.makeText(this, text, Toast.LENGTH_SHORT);
    toast.show();
  }
}
