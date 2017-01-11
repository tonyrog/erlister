//
// SPRINGBACK button output (toggle)
//
machine springback;

in button;

out value = on;

states off, on;

trans

off: on  button;
on:  off button;
