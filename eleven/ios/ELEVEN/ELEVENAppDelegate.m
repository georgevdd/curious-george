//
//  ELEVENAppDelegate.m
//  ELEVEN
//
//  Created by George van den Driessche on 2/27/14.
//  Copyright (c) 2014 George van den Driessche. All rights reserved.
//

#import "ELEVENAppDelegate.h"
#import "FacebookSDK/FBAppCall.h"
#import "FacebookSDK/FBLoginView.h"
#import "FacebookSDK/FBProfilePictureView.h"
#import "FacebookSDK/FBRequest.h"
#import <Parse/Parse.h>

@implementation ELEVENAppDelegate

- (BOOL)application:(UIApplication *)application
didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    [FBLoginView class];
    [FBProfilePictureView class];

    [Parse setApplicationId:@"BcfLhnv2Se2tHjgdMkuxh9NsbPy3pXXdLTs3z7W1"
                  clientKey:@"CISlYJCXoSl4dAo16XSv0xvWeaMQdJO9JhuLCnyk"];
    [PFAnalytics trackAppOpenedWithLaunchOptions:launchOptions];
    [PFFacebookUtils initializeFacebook];

    return YES;
}

- (BOOL)application:(UIApplication *)application
            openURL:(NSURL *)url
  sourceApplication:(NSString *)sourceApplication
         annotation:(id)annotation {
    BOOL wasHandled = [FBAppCall handleOpenURL:url
                             sourceApplication:sourceApplication
                                   withSession:[PFFacebookUtils session]];
    return wasHandled;
}

- (void)applicationWillResignActive:(UIApplication *)application
{
    // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
    // Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
}

- (void)applicationDidEnterBackground:(UIApplication *)application
{
    // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later. 
    // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
    
}

- (void)applicationWillEnterForeground:(UIApplication *)application
{
    // Called as part of the transition from the background to the inactive state; here you can undo many of the changes made on entering the background.
}

- (void)applicationDidBecomeActive:(UIApplication *)application
{
    [FBAppCall handleDidBecomeActiveWithSession:[PFFacebookUtils session]];
}

- (void)applicationWillTerminate:(UIApplication *)application
{
    // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
}

@end
