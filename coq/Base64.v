Require Import List.
Require Import Ascii.
Require Import Grp.
Require Import ListUtil.
Require Import Vec.

Definition ascii_of_bool6 (bs: Vec bool 6) : ascii.
 intro bs.
 inversion bs.
 inversion H1.
 inversion H4.
 inversion H7.
 inversion H10.
 inversion H13.
 exact (Ascii H15 H12 H9 H6 H3 H0 false false).
Defined.

Definition bool6_of_ascii (c : ascii) : (Vec bool 6) :=
  match c with
    | Ascii a b c d e f _ _ =>
      vec_of_list 6 (f::e::d::c::b::a::nil) (refl_equal _)
  end.

Lemma ascii_bool6_inv : forall bs : Vec bool 6,
  (bool6_of_ascii (ascii_of_bool6 bs)) = bs.
Proof.
Admitted.

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
Defined.

Require Import Arith.
Definition le_lt_dec_aux: forall x y:nat, {x <= y} + {~x <= y}.
 refine (fun x y =>
   if le_lt_dec x y then
     left _ _
   else
     right _ _).
  exact _H.

  exact (lt_not_le y x _H).
Defined.
Infix "<=?" := le_lt_dec_aux (at level 60).
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

Definition tblaux_inv (n: nat) : nat :=
  if 97 <=? n then (* n is ['a'..'z'] *)
    26 + n - nat_of_ascii "a"
  else if 65 <=? n then (* n is ['A'..'Z'] *)
    n - nat_of_ascii "A"
  else if 48 <=? n then (* n is ['0'..'9'] *)
    52 + n - nat_of_ascii "0"
  else if n =? nat_of_ascii "+" then
    62
  else (* if n = nat_of_ascii "/" *) 
    63.

Definition tbl_inv (c: ascii) : Vec bool 6 :=
  bool6_of_ascii (ascii_of_nat (tblaux_inv (nat_of_ascii c))).

Definition digit (char:ascii) : Vec bool 8 :=
  match char with
    | Ascii a b c d e f g h =>
      vec_of_list 8 (h::g::f::e::d::c::b::a::nil) (refl_equal _)
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

Axiom mlstring_of_asciilist_inv : forall cs, asciilist_of_mlstring (mlstring_of_asciilist cs) = cs.
Axiom asciilist_of_mlstring_inv : forall s,  mlstring_of_asciilist (asciilist_of_mlstring s) = s.

Definition encode (s : mlstring) : mlstring :=
  mlstring_of_asciilist (cm (gfill "="%char 4 (map tbl (gfill false 6 (cm (map digit (asciilist_of_mlstring s))) (lt_O_Sn _))) (lt_O_Sn _))).
(*  string_of_list (cm (gfill "="%char 4 (map tbl (gfill false 6 (cm (map digit (list_of_string s))) (lt_O_Sn _))) (lt_O_Sn _))).*)

Definition decode (s: mlstring) : mlstring :=
  mlstring_of_asciilist (map ascii_of_bool8 (gcut 8 (cm (map tbl_inv
    (delete ascii_dec "="%char (asciilist_of_mlstring s))))(lt_O_Sn _))).
(*  string_of_list (map ascii_of_bool8 (gcut 8 (cm (map tbl_inv
    (delete ascii_dec "="%char (list_of_string s))))(lt_O_Sn _))).*)

Lemma delete_gfill: forall d n H cs,
  ~In d cs -> delete ascii_dec d (cm (gfill d n cs H)) = cs.
Proof.
 intros; simpl.
 unfold gfill.
 rewrite cm_dist.
 simpl cm at 2.
 rewrite list_vec.
 rewrite <- app_nil_end.
 rewrite <- app_ass.
 rewrite cm_g1_g2.
 rewrite delete_dist.
 rewrite (delete_notIn _ _ _ H0).
 rewrite delete_replicate.
 rewrite <- app_nil_end; reflexivity.
Qed.

Lemma ascii_nat_embedding_inv : forall n,
  n < 256 -> nat_of_ascii (ascii_of_nat n) = n.
Admitted.

Require Omega.
Lemma tblaux_lt_256 : forall n:nat, tblaux n < 256.
Proof.
 unfold tblaux; intro n.
 destruct (n <=? 25);
   [ | destruct (n <=? 51);
     [ | destruct (n <=? 61);
       [ | destruct (n =? 62)
       ]]]; simpl; omega.
Qed.

Lemma sumbool_rewrite : forall {P: Prop} (b: {P}+{~P}),
  P -> exists H, b = left _ H.
Proof.
 intros P b H.
 destruct b.
  exists p; reflexivity.

  elim n; apply H.
Qed.

Lemma sumbool_rewrite_not : forall {P: Prop} (b: {P}+{~P}),
  ~P -> exists H, b = right _ H.
Proof.
 intros P b H.
 destruct b.
  elim H; apply p.

  exists n; reflexivity.
Qed.
 
Lemma tblaux_inv_inv: forall n: nat,
  n < 64 -> tblaux_inv (tblaux n) = n.
Proof.
 intros n H.
 unfold tblaux.
 destruct (n <=? 25).
  unfold tblaux_inv.
  elim (sumbool_rewrite_not (97 <=? nat_of_ascii "A" + n)); [|simpl; omega].
  intros _H0 eq; rewrite eq.
  elim (sumbool_rewrite (65 <=? nat_of_ascii "A" + n)); [|simpl; omega].
  intros _H1 eq1; rewrite eq1.
  simpl; omega.

  destruct (n <=? 51).
   unfold tblaux_inv.
   elim (sumbool_rewrite (97 <=? nat_of_ascii "a" + (n - 26))); [|simpl; omega].
   intros _H0 eq0; rewrite eq0.
   simpl; omega.

   destruct (n <=? 61).
    unfold tblaux_inv.
    elim (sumbool_rewrite_not (97 <=? nat_of_ascii "0" + (n-52))); [|simpl; omega].
    intros _H0 eq; rewrite eq.
    elim (sumbool_rewrite_not (65 <=? nat_of_ascii "0" + (n-52))); [|simpl; omega].
    intros _H1 eq1; rewrite eq1.
    elim (sumbool_rewrite (48 <=? nat_of_ascii "0" + (n-52))); [|simpl; omega].
    intros _H2 eq2; rewrite eq2.
    simpl; omega.
    
    destruct (n =? 62).
     rewrite e; reflexivity.

     unfold tblaux_inv; simpl. omega.
Qed.

Lemma nat_of_bool6_lt_64: forall bs: Vec bool 6,
  nat_of_ascii (ascii_of_bool6 bs) < 64.
Admitted.

Lemma tbl_inv_inv: forall bs: Vec bool 6, tbl_inv (tbl bs) = bs.
Proof.
 unfold tbl.
 unfold tbl_inv.
 intro; rewrite ascii_nat_embedding_inv.
  rewrite tblaux_inv_inv.
   rewrite ascii_nat_embedding.
    apply ascii_bool6_inv.
    
    apply nat_of_bool6_lt_64.

   apply tblaux_lt_256.
Qed.
 
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
 rewrite mlstring_of_asciilist_inv.
 rewrite delete_gfill.
  rewrite map_map.
  rewrite (map_ext  _ (fun x => x) tbl_inv_inv).
  rewrite map_id.
  rewrite grp_inv; [ | auto].
  rewrite map_map.
  rewrite (map_ext _ (fun x => x) digit_inv).
  rewrite map_id.
  rewrite asciilist_of_mlstring_inv.
  reflexivity.

  apply tbl_notIn_eq.
Qed.

Definition mlstring_of_string s := mlstring_of_asciilist (list_of_string s).

Extraction "base64.ml" encode decode.