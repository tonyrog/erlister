//
// Simple errors
//

machine already;

in x, y;
in a, y;

out z = x;
out z = a;

clocks t1 = uptime[0-10, 1] 2;
clocks t1 = downtime[0-10, 1] 2;

states s1, s2, s3, s1, a;
