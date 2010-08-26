Require Import List.
Require Import Ascii.
Require Import Grp.
Require Import ListUtil.
Require Import Vec.
(*
Variable gfill : forall{A:Set}(default:A)(n:nat) (xs:list A) (H:0<n), list (Vec A n).
Variable gcut : forall{A:Set} (default:A)n (xs:list A) (H:0<n), list(Vec A n).
Variable cm : forall {A:Set}{n:nat} (vs :list (Vec A n)), list A.
*)

(*
Axiom grp_inv: forall {A:Set}(default:A) n m H0 H1 (vs: list (Vec A m)),
  n < m -> gcut default m (cm (gfill default n (cm vs) H0)) H1 = vs.
*)
Definition ascii_of_bool6 (bs: Vec bool 6) : ascii.
 intro bs.
 inversion bs.
 inversion H1.
 inversion H4.
 inversion H7.
 inversion H10.
 inversion H13.
(* exact (Ascii false false H0 H3 H6 H9 H12 H15).*)
 exact (Ascii H15 H12 H9 H6 H3 H0 false false).
Defined.

Definition ascii_of_bool8 (bs: Vec bool 8) : ascii.
 intro bs.
 inversion bs.
 inversion H1.
 inversion H4.
 inversion H7.
 inversion H10.
 inversion H13.
 inversion H16.
 inversion H19.
 exact (Ascii H21 H18 H15 H12 H9 H6 H3 H0).
(* exact (Ascii H0 H3 H6 H9 H12 H15 H18 H21).*)
Defined.

Require Import Arith.
Infix "<=?" := le_lt_dec (at level 60).
Infix "=?" := eq_nat_dec (at level 60).

Definition tblaux (n: nat) : nat :=
  if n <=? 25 then
    nat_of_ascii "A" + n
  else if n <=? 51 then
    nat_of_ascii "a" + (n - 26)
  else if n <=? 61 then
    nat_of_ascii "0" + (n - 52)
  else if n =? 62 then
    nat_of_ascii "+"
  else (*if n =? 63 then*)
    nat_of_ascii "/".

Definition tbl (bs: Vec bool 6) : ascii :=
  ascii_of_nat (tblaux (nat_of_ascii (ascii_of_bool6 bs))).

Variable tbl_inv : (ascii) -> Vec bool 6.

Definition digit (char:ascii) : Vec bool 8 :=
  match char with
    | Ascii a b c d e f g h => vec_of_list 8 (h::g::f::e::d::c::b::a::nil) (refl_equal _)
(*    | Ascii a b c d e f g h => vec_of_list 8 (a::b::c::d::e::f::g::h::nil) (refl_equal _)*)
  end.

Definition digits : list ascii -> list (Vec bool 8) := map digit.
Require Import String.
(*Fixpoint list_of_string s :=
  match s with
    | EmptyString => nil
    | String c s => c::list_of_string s
  end.
Fixpoint string_of_list cs :=
  match cs with
    | nil => EmptyString
    | c::cs => String c (string_of_list cs)
  end.
*)
Require Import Extr.

Axiom mlstring_inv : forall cs, asciilist_of_mlstring (mlstring_of_asciilist cs) = cs.
Axiom mlstring_inv2: forall s,  mlstring_of_asciilist (asciilist_of_mlstring s) = s.


Definition encode (s : mlstring) : mlstring :=
  mlstring_of_asciilist (cm (gfill "="%char 4 (map tbl (gfill false 6 (cm (map digit (asciilist_of_mlstring s))) (lt_O_Sn _))) (lt_O_Sn _))).
(*  string_of_list (cm (gfill "="%char 4 (map tbl (gfill false 6 (cm (map digit (list_of_string s))) (lt_O_Sn _))) (lt_O_Sn _))).*)

Definition decode (s: mlstring) : mlstring :=
  mlstring_of_asciilist (map ascii_of_bool8 (gcut 8 (cm (map tbl_inv
    (delete ascii_dec "="%char (asciilist_of_mlstring s))))(lt_O_Sn _))).
(*  string_of_list (map ascii_of_bool8 (gcut 8 (cm (map tbl_inv
    (delete ascii_dec "="%char (list_of_string s))))(lt_O_Sn _))).*)


Axiom delete_gfill: forall d n cs H,
~In d cs -> delete ascii_dec d (cm (gfill d n cs H)) = cs.

Axiom tbl_inv_inv: forall c, tbl_inv (tbl c) = c.

Lemma digit_inv: forall c, ascii_of_bool8 (digit c) = c.
Proof.
 destruct c; reflexivity.
Qed.

Axiom tbl_not_eq : forall v, tbl v <> "="%char.
(*Proof.
 intro v; unfold tbl.
 unfold tblaux.
 destruct (ascii_of_bool6 v).
*)


Lemma tbl_notIn_eq : forall vs, ~ In "="%char (map tbl vs).
Proof.
 induction vs; simpl; intro; [elim H |].
 destruct H.
  elim (tbl_not_eq a H).

  elim (IHvs H).
Qed.

Theorem soundness : forall s, decode (encode s) = s.
Proof.
 intro s; unfold encode; unfold decode.
 rewrite mlstring_inv.
 rewrite delete_gfill.
  rewrite map_map.
  rewrite (map_ext  _ (fun x => x) tbl_inv_inv).
  rewrite map_id.
  rewrite grp_inv; [ | auto].
  rewrite map_map.
  rewrite (map_ext _ (fun x => x) digit_inv).
  rewrite map_id.
  rewrite mlstring_inv2.
  reflexivity.

  apply tbl_notIn_eq.
Qed.

Definition mlstring_of_string s := mlstring_of_asciilist (list_of_string s).

Extraction "base64.ml" encode.

