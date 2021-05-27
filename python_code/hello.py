from datetime import datetime
import sys
import caldav

#https://nextcloud.r-podcastdev.link/index.php/apps/calendar/p/zNyH2RE3pLHctoEf

#       "https://nextcloud03.webo.cloud/remote.php/dav/calendars/me@myweb.de/personal/"
#       "https://nextcloud.r-podcastdev.link/remote.php/dav/calendars/rpodcast/wimpys-world-of-streamers/"
#caldav_url = 'https://calendar.example.com/dav'
caldav_url = 'https://nextcloud.r-podcastdev.link/remote.php/dav'

# substitute for real user name and password (preferebly from env vars)
username = 'rpodcast'
password = 'mypassword'

client = caldav.DAVClient(url=caldav_url, username=username, password=password)
my_principal = client.principal()
calendars = my_principal.calendars()

wimpy_cal = "https://nextcloud.r-podcastdev.link/remote.php/dav/calendars/rpodcast/wimpys-world-of-streamers/"

the_same_calendar = client.calendar(url=wimpy_cal)

all_events = the_same_calendar.events()

if all_events:
    print("your calendar has %i cevents:" % len(all_events))
    for e in all_events:
        print(e)
        print(e.data)
else:
    print("your calendar has no events")


if calendars:
    ## Some calendar servers will include all calendars you have
    ## access to in this list, and not only the calendars owned by
    ## this principal.
    print("your principal has %i calendars:" % len(calendars))
    for c in calendars:
        print("    Name: %-20s  URL: %s" % (c.name, c.url))
else:
    print("your principal has no calendars")

