//
// SPRINGBACK button output (toggle)
//
machine springback;

in button;

out value = ON;

states OFF, ON;

trans

OFF: ON  button;
ON:  OFF button;
