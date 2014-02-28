//
//  ELEVENViewController.m
//  ELEVEN
//
//  Created by George van den Driessche on 2/27/14.
//  Copyright (c) 2014 George van den Driessche. All rights reserved.
//

#import "ELEVENViewController.h"

#import "FacebookSDK/FBProfilePictureView.h"
#import <Parse/Parse.h>

@interface ELEVENViewController ()
@property (strong, nonatomic) IBOutlet FBProfilePictureView *profilePictureView;
@property (strong, nonatomic) IBOutlet UILabel *nameLabel;
@property (strong, nonatomic) IBOutlet FBLoginView *loginView;

@end

@implementation ELEVENViewController

- (void)viewDidLoad
{
    [super viewDidLoad];

    NSArray* permissions = @[@"basic_info", @"user_friends"];
    [PFFacebookUtils logInWithPermissions:permissions block:^(PFUser *user, NSError *error) {
        if (!user) {
            NSLog(@"Facebook login didn't happen.");
        } else {
            [FBRequestConnection startForMeWithCompletionHandler:^(FBRequestConnection *connection, id result, NSError *error) {
                NSLog(@"user info: %@", result);
                NSLog(@"User ID: %@", [result id]);
                self.profilePictureView.profileID = [result id];
                self.nameLabel.text = [result name];
            }];
        }
    }];
}

@end
