/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package org.mozilla.gecko.fxa.activities;

import java.io.IOException;
import java.util.Map;

import org.mozilla.gecko.R;
import org.mozilla.gecko.background.common.log.Logger;
import org.mozilla.gecko.background.fxa.FxAccountClient10.RequestDelegate;
import org.mozilla.gecko.background.fxa.FxAccountClient20.LoginResponse;
import org.mozilla.gecko.background.fxa.FxAccountClientException.FxAccountClientRemoteException;
import org.mozilla.gecko.background.fxa.FxAccountUtils;
import org.mozilla.gecko.background.fxa.PasswordStretcher;
import org.mozilla.gecko.fxa.FxAccountConstants;
import org.mozilla.gecko.fxa.activities.FxAccountSetupTask.ProgressDisplay;
import org.mozilla.gecko.fxa.authenticator.AndroidFxAccount;
import org.mozilla.gecko.fxa.login.Engaged;
import org.mozilla.gecko.fxa.login.State;
import org.mozilla.gecko.sync.SyncConfiguration;
import org.mozilla.gecko.sync.setup.Constants;

import android.accounts.AccountManager;
import android.content.Intent;
import android.text.Editable;
import android.text.InputType;
import android.text.TextWatcher;
import android.util.Patterns;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnFocusChangeListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.TextView.OnEditorActionListener;

abstract public class FxAccountAbstractSetupActivity extends FxAccountAbstractActivity implements ProgressDisplay {
  public FxAccountAbstractSetupActivity() {
    super(CANNOT_RESUME_WHEN_ACCOUNTS_EXIST | CANNOT_RESUME_WHEN_LOCKED_OUT);
  }

  protected FxAccountAbstractSetupActivity(int resume) {
    super(resume);
  }

  private static final String LOG_TAG = FxAccountAbstractSetupActivity.class.getSimpleName();

  protected int minimumPasswordLength = 8;

  protected EditText emailEdit;
  protected EditText passwordEdit;
  protected Button showPasswordButton;
  protected TextView remoteErrorTextView;
  protected Button button;
  protected ProgressBar progressBar;

  protected void createShowPasswordButton() {
    showPasswordButton.setOnClickListener(new OnClickListener() {
      @SuppressWarnings("deprecation")
      @Override
      public void onClick(View v) {
        boolean isShown = 0 == (passwordEdit.getInputType() & InputType.TYPE_TEXT_VARIATION_PASSWORD);
        // Changing input type loses position in edit text; let's try to maintain it.
        int start = passwordEdit.getSelectionStart();
        int stop = passwordEdit.getSelectionEnd();
        passwordEdit.setInputType(passwordEdit.getInputType() ^ InputType.TYPE_TEXT_VARIATION_PASSWORD);
        passwordEdit.setSelection(start, stop);
        if (isShown) {
          showPasswordButton.setText(R.string.fxaccount_password_show);
          showPasswordButton.setBackgroundDrawable(getResources().getDrawable(R.drawable.fxaccount_password_button_show_background));
          showPasswordButton.setTextColor(getResources().getColor(R.color.fxaccount_password_show_textcolor));
        } else {
          showPasswordButton.setText(R.string.fxaccount_password_hide);
          showPasswordButton.setBackgroundDrawable(getResources().getDrawable(R.drawable.fxaccount_password_button_hide_background));
          showPasswordButton.setTextColor(getResources().getColor(R.color.fxaccount_password_hide_textcolor));
        }
      }
    });
  }

  protected void hideRemoteError() {
    remoteErrorTextView.setVisibility(View.INVISIBLE);
  }

  protected void showRemoteError(Exception e, int defaultResourceId) {
    if (e instanceof IOException) {
      remoteErrorTextView.setText(R.string.fxaccount_remote_error_COULD_NOT_CONNECT);
    } else if (e instanceof FxAccountClientRemoteException) {
      remoteErrorTextView.setText(((FxAccountClientRemoteException) e).getErrorMessageStringResource());
    } else {
      remoteErrorTextView.setText(defaultResourceId);
    }
    Logger.warn(LOG_TAG, "Got exception; showing error message: " + remoteErrorTextView.getText().toString(), e);
    remoteErrorTextView.setVisibility(View.VISIBLE);
  }

  protected void addListeners() {
    TextChangedListener textChangedListener = new TextChangedListener();
    EditorActionListener editorActionListener = new EditorActionListener();
    FocusChangeListener focusChangeListener = new FocusChangeListener();

    emailEdit.addTextChangedListener(textChangedListener);
    emailEdit.setOnEditorActionListener(editorActionListener);
    emailEdit.setOnFocusChangeListener(focusChangeListener);
    passwordEdit.addTextChangedListener(textChangedListener);
    passwordEdit.setOnEditorActionListener(editorActionListener);
    passwordEdit.setOnFocusChangeListener(focusChangeListener);
  }

  protected class FocusChangeListener implements OnFocusChangeListener {
    @Override
    public void onFocusChange(View v, boolean hasFocus) {
      if (hasFocus) {
        return;
      }
      updateButtonState();
    }
  }

  protected class EditorActionListener implements OnEditorActionListener {
    @Override
    public boolean onEditorAction(TextView v, int actionId, KeyEvent event) {
      updateButtonState();
      return false;
    }
  }

  protected class TextChangedListener implements TextWatcher {
    @Override
    public void afterTextChanged(Editable s) {
      updateButtonState();
    }

    @Override
    public void beforeTextChanged(CharSequence s, int start, int count, int after) {
      // Do nothing.
    }

    @Override
    public void onTextChanged(CharSequence s, int start, int before, int count) {
      // Do nothing.
    }
  }

  protected boolean shouldButtonBeEnabled() {
    final String email = emailEdit.getText().toString();
    final String password = passwordEdit.getText().toString();

    boolean enabled =
        (email.length() > 0) &&
        Patterns.EMAIL_ADDRESS.matcher(email).matches() &&
        (password.length() >= minimumPasswordLength);
    return enabled;
  }

  protected boolean updateButtonState() {
    boolean enabled = shouldButtonBeEnabled();
    if (!enabled) {
      // The user needs to do something before you can interact with the button;
      // presumably that interaction will fix whatever error is shown.
      hideRemoteError();
    }
    if (enabled != button.isEnabled()) {
      Logger.debug(LOG_TAG, (enabled ? "En" : "Dis") + "abling button.");
      button.setEnabled(enabled);
    }
    return enabled;
  }

  @Override
  public void showProgress() {
    progressBar.setVisibility(View.VISIBLE);
    button.setVisibility(View.INVISIBLE);
  }

  @Override
  public void dismissProgress() {
    progressBar.setVisibility(View.INVISIBLE);
    button.setVisibility(View.VISIBLE);
  }

  public Intent makeSuccessIntent(String email, LoginResponse result) {
    Intent successIntent;
    if (result.verified) {
      successIntent = new Intent(this, FxAccountVerifiedAccountActivity.class);
    } else {
      successIntent = new Intent(this, FxAccountConfirmAccountActivity.class);
    }
    // Per http://stackoverflow.com/a/8992365, this triggers a known bug with
    // the soft keyboard not being shown for the started activity. Why, Android, why?
    successIntent.setFlags(Intent.FLAG_ACTIVITY_NO_ANIMATION);
    return successIntent;
  }

  protected abstract class AddAccountDelegate implements RequestDelegate<LoginResponse> {
    public final String email;
    public final PasswordStretcher passwordStretcher;
    public final String serverURI;
    public final Map<String, Boolean> selectedEngines;

    public AddAccountDelegate(String email, PasswordStretcher passwordStretcher, String serverURI) {
      this(email, passwordStretcher, serverURI, null);
    }

    public AddAccountDelegate(String email, PasswordStretcher passwordStretcher, String serverURI, Map<String, Boolean> selectedEngines) {
      if (email == null) {
        throw new IllegalArgumentException("email must not be null");
      }
      if (passwordStretcher == null) {
        throw new IllegalArgumentException("passwordStretcher must not be null");
      }
      if (serverURI == null) {
        throw new IllegalArgumentException("serverURI must not be null");
      }
      this.email = email;
      this.passwordStretcher = passwordStretcher;
      this.serverURI = serverURI;
      // selectedEngines can be null, which means don't write
      // userSelectedEngines to prefs. This makes any created meta/global record
      // have the default set of engines to sync.
      this.selectedEngines = selectedEngines;
    }

    @Override
    public void handleSuccess(LoginResponse result) {
      Logger.info(LOG_TAG, "Got success response; adding Android account.");

      // We're on the UI thread, but it's okay to create the account here.
      AndroidFxAccount fxAccount;
      try {
        final String profile = Constants.DEFAULT_PROFILE;
        final String tokenServerURI = FxAccountConstants.DEFAULT_TOKEN_SERVER_ENDPOINT;
        // It is crucial that we use the email address provided by the server
        // (rather than whatever the user entered), because the user's keys are
        // wrapped and salted with the initial email they provided to
        // /create/account. Of course, we want to pass through what the user
        // entered locally as much as possible, so we create the Android account
        // with their entered email address, etc.
        // The passwordStretcher should have seen this email address before, so
        // we shouldn't be calculating the expensive stretch twice.
        byte[] quickStretchedPW = passwordStretcher.getQuickStretchedPW(result.remoteEmail.getBytes("UTF-8"));
        byte[] unwrapkB = FxAccountUtils.generateUnwrapBKey(quickStretchedPW);
        State state = new Engaged(email, result.uid, result.verified, unwrapkB, result.sessionToken, result.keyFetchToken);
        fxAccount = AndroidFxAccount.addAndroidAccount(getApplicationContext(),
            email,
            profile,
            serverURI,
            tokenServerURI,
            state);
        if (fxAccount == null) {
          throw new RuntimeException("Could not add Android account.");
        }

        if (selectedEngines != null) {
          Logger.info(LOG_TAG, "User has selected engines; storing to prefs.");
          SyncConfiguration.storeSelectedEnginesToPrefs(fxAccount.getSyncPrefs(), selectedEngines);
        }
      } catch (Exception e) {
        handleError(e);
        return;
      }

      // For great debugging.
      if (FxAccountConstants.LOG_PERSONAL_INFORMATION) {
        fxAccount.dump();
      }

      // The GetStarted activity has called us and needs to return a result to the authenticator.
      final Intent intent = new Intent();
      intent.putExtra(AccountManager.KEY_ACCOUNT_NAME, email);
      intent.putExtra(AccountManager.KEY_ACCOUNT_TYPE, FxAccountConstants.ACCOUNT_TYPE);
      // intent.putExtra(AccountManager.KEY_AUTHTOKEN, accountType);
      setResult(RESULT_OK, intent);

      // Show success activity depending on verification status.
      Intent successIntent = makeSuccessIntent(email, result);
      startActivity(successIntent);
      finish();
    }
  }
}
