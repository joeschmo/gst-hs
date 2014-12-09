zero = z
one = s(z)
two = s(s(z))

idnat = fn (n : nat) n
succ = fn (n : nat) s(n)
pred = fn (m : nat) natrec m {z => z | s(x) with r => x}
sum = fn (m : nat) natrec m {z => idnat | s(x) with r => fn (n : nat) s(r(n))}
subs = fn (m : nat) fn (n : nat) natrec n {z => m | s(x) with r => pred(r)}
mult = fn (m : nat) natrec m {z => fn (n : nat) z | s(x) with r => fn (n : nat) sum(n)(r(n))}
ackermann = fn (m : nat) natrec m {z => succ | s(x) with r1 => fn (n : nat) natrec n {z => r1(s(z)) | s(x) with r2 => r1(r2)}}
