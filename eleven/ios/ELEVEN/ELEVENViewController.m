//
//  ELEVENViewController.m
//  ELEVEN
//
//  Created by George van den Driessche on 2/27/14.
//  Copyright (c) 2014 George van den Driessche. All rights reserved.
//

#import "ELEVENViewController.h"

#import "FacebookSDK/FBProfilePictureView.h"

@interface ELEVENViewController ()
@property (strong, nonatomic) IBOutlet FBProfilePictureView *profilePictureView;
@property (strong, nonatomic) IBOutlet UILabel *nameLabel;
@property (strong, nonatomic) IBOutlet UILabel *statusLabel;
@property (strong, nonatomic) IBOutlet FBLoginView *loginView;

@end

@implementation ELEVENViewController

- (void)viewDidLoad
{
    [super viewDidLoad];
	self.loginView.readPermissions = @[@"basic_info", @"user_friends"];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

// Logged-in user experience
- (void)loginViewShowingLoggedInUser:(FBLoginView *)loginView {
    self.statusLabel.text = @"You're logged in as";
}

// Logged-out user experience
- (void)loginViewShowingLoggedOutUser:(FBLoginView *)loginView {
    self.profilePictureView.profileID = nil;
    self.nameLabel.text = @"";
    self.statusLabel.text= @"You're not logged in!";
}

// This method will be called when the user information has been fetched
- (void)loginViewFetchedUserInfo:(FBLoginView *)loginView
                            user:(id<FBGraphUser>)user {
    self.profilePictureView.profileID = user.id;
    self.nameLabel.text = user.name;
}

@end
