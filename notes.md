```
class*: superclass does not provide an expected method for override
  override name: on-char
  class name: calculator-panel%
```

this means that the superclass doesn't have a method of that name to
override. Like, in this case, the superclass is `panel%` and this
class doesn't have a `on-char` method to override, so I got the above
error message.

```
(define calculator-panel%
  (class panel%
    (define/override (on-char event) (void))
    (super-new)))
```

[key event description](file:///usr/share/doc/racket/gui/key-event_.html?q=gui)

to get the pressed key: `(send key-event get-key-code)`. For normal
characters, the key code is the racket character for that character.
So `\#a` should represent 'a' being pressed.

The arguments received by an event handler are documented where it says how
the event handler will be called. For `on-subwindow-char`

```
(send a-window on-subwindow-char receiver event)
  receiver : (is-a?/c window<%>)
  event : (is-a?/c key-event%
```

so I need to create an override that accepts two arguments

I open the calc and try to press keyboard keys and get nothing. maybe
my panel isn't focused? I try to press tab and I see the error msg
notifying me that my method accepts too few arguments. I think Tab
changed focus.

weird... I am seeing pressed keys... but only when I quit the program.
almost as if my event handler never terminates.

There's notes about the meaning of returned boolean. Maybe I have to
return a boolean? Let's try returning `#f`. Nope, nothing. Let's try
`#t`. Nope, that doesn't work either.

Ah, there's nothing wrong with my handler, it runs. It just keeps
clearing my calculator bc it returns characters, and not symbols.

Last problem is that the calculator-panel% doesn't start off with
focus. Hm. These are all really, really great things to deal with on
an obvious project as opposed to the less obvious conversation thing.

macro stepper tutorials

- <http://www.ccs.neu.edu/home/ryanc/macro-stepper/tutorial.html>
- <http://www.ccs.neu.edu/home/ryanc/macro-stepper/macro-stepper.html>
- <https://stackoverflow.com/questions/8961762/macro-stepper-in-drracket>
