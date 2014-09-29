Deathtoll
=========

Naive monitoring of remote nodes and alerting when they are going down.

Available under the terms of [MPL 2.0](LICENSE) license.

How to use
----------

There are two ways:

 - through the `watch` env variable, see:

```erlang
  {deathtoll, [
      {watch, [
          {google, [
              {auditor, {deathtoll_audit_http, [
                  {url, "https://www.google.ru/"},
                  {expect, {status, 200}}
              ]}},
              {alarms, [
                  {deathtoll_alarm_logger, []}
              ]},
              {interval, 8}
          ]}
      ]}
  ]}
```

 - through the call to `deathtoll:watch/2`, much the same thing but with bare hands:

```erlang
deathtoll:watch(
  watch,
  [
      {google, [
          {auditor, {deathtoll_audit_http, [
              {url, "https://www.google.ru/"},
              {expect, {status, 200}}
          ]}},
          {alarms, [
              {deathtoll_alarm_logger, []}
          ]},
          {interval, 8}
      ]}
  ]
).
```

You can `deathtoll:unwatch/1` too, yay!

What’s the thing
----------------

Every ’watch’ is a combination of an auditor, up to a few alarmists each of which could provide means to set a formatter. Moreover, every ’watch’ is hell of a supervior.

There is a couple of options you, as a proud user, could pass to each component. Sad to say, there are yet to be documented. Meanwhile you could consult a couple of modules, there are specs all over.


