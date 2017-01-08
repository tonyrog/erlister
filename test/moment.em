//
// MOMENT button output
//
machine moment;

in button;

out value = ON;

states OFF, ON;

trans

OFF: ON  !button;
ON:  OFF button;
