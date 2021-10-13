$( document ).ready(function() {
  var d = new Date();
  var target = $('#clientTime');
  var offset = $('#clientTimeOffset');
  var timezone = $('#clientZone')

  offset.val(d.getTimezoneOffset());
  offset.trigger("change");
  target.val(d.toLocaleString());
  target.trigger("change");

  timezone.val(Intl.DateTimeFormat().resolvedOptions().timeZone);
  timezone.trigger("change")
});
