# shinycal 0.7

* Add custom javascript for detecting the time zone of the user browsing the app, and make that the default time zone when application finishes loading. This solves the bug of time zones not being correctly mapped for those outside of the original America/New_York time zone (#26)

# shinycal 0.6

* Account for daylight savings time by adding `ambiguous` param to custom time parser functions
* Add nodejs and puppeteer to the development contrainer for further timezone testing

# shinycal 0.5.1

* Organize the time zone choices into groups (thanks, @tanho63 and @PythonCoderUnicorn!)
# shinycal 0.5

* Add a new time zone selector (powered by `{shinyWidgets}`) 
* Revise how the view type changes the calendar view on the backend. The value of this input is now directly fed into the calendar function, instead of changing it via the proxy object.
# shinycal 0.4

* Move video player for a streamer's latest VOD to the right of the calendar
# shinycal 0.3

* Added a `NEWS.md` file to track changes to the package.
* Allow user to change the calendar entry background color as well as font color
* Add placeholder margin for new elements
* Restructured reactive components of the calendar display data
