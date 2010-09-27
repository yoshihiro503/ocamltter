Require Import Arith.
Require Import List.
Require Import String.
Require Import Ascii.
Require Import Grp.
Require Import ListUtil.
Require Import Vec.
Require Import JMeq.
Require Import Omega.

Definition ascii_of_bool6 (bs: Vec bool 6) : ascii :=
  match bs with
    | VCons 5 b1 (VCons 4 b2 (VCons 3 b3 (VCons 2 b4 (VCons 1 b5 (VCons 0 b6 VNil))))) => Ascii b6 b5 b4 b3 b2 b1 false false
    | _  => "-"%char (* dummy *)
  end.
(*
 intro bs.
 inversion bs.
 inversion H1.
 inversion H4.
 inversion H7.
 inversion H10.
 inversion H13.
 exact (Ascii H15 H12 H9 H6 H3 H0 false false).
Defined.
*)

Definition bool6_of_ascii (c : ascii) : (Vec bool 6) :=
  match c with
    | Ascii a b c d e f _ _ =>
      vec_of_list 6 (f::e::d::c::b::a::nil) (refl_equal _)
  end.

Lemma not_jmeq : forall {A B:Type} (x:A) (y:B), A <> B -> ~JMeq x y.
intros.
intro; elim H.
inversion H0.
reflexivity.
Qed.

Lemma vec_eq : forall {A:Type} (m n:nat), Vec A m = Vec A n -> m = n.
Admitted. (*未解決問題*)

Lemma vec_neq : forall {A:Type} (m n:nat), m <> n -> Vec A m <> Vec A n.
 intros.
 intro; elim H.
 apply (vec_eq m n H0).
Qed.

Lemma veccase1 : forall {A:Type} (v:Vec A 0),
  v = VNil.
Proof.
 intros.
 refine (match v as x return JMeq v x -> v = VNil
           with VNil => _ | VCons n y vv => _ end (JMeq_refl _)).
  intro H.
  rewrite (JMeq_eq H); reflexivity.

  intro H.
  eelim (not_jmeq v (VCons n y vv)); [| apply H].
  apply vec_neq; apply O_S.
Qed.
 
Lemma veccase2 : forall {A:Type} n (v:Vec A (S n)),
 exists x:A, exists w, v = (VCons n x w).
Proof.
 intros.
 refine (match v as x return JMeq v x -> exists x:A, exists w, v = (VCons n x w)
           with VNil => _ | VCons n y vv => _ end (JMeq_refl _)).
  intro H; eelim (not_jmeq v (@VNil A)); [| apply H].
  apply vec_neq; apply sym_not_eq; apply O_S.

  intro H.
  inversion H.
  revert v y vv H H3.
  injection (vec_eq (S n) (S n0) H1); intro eq.
  rewrite eq.
  intros.
  exists y; exists vv.
  rewrite (JMeq_eq H).
  reflexivity.
Qed.
 
Lemma ascii_bool6_inv : forall bs : Vec bool 6,
  (bool6_of_ascii (ascii_of_bool6 bs)) = bs.
Proof.
 intros.
 elim (veccase2 5 bs); intros b1 _H; destruct _H as [bs' eq1]; rewrite eq1;
   clear eq1 bs.
 elim (veccase2 4 bs'); intros b2 _H; destruct _H as [bs eq]; rewrite eq;
   clear eq bs'.
 elim (veccase2 3 bs); intros b3 _H; destruct _H as [bs' eq]; rewrite eq;
   clear eq bs.
 elim (veccase2 2 bs'); intros b4 _H; destruct _H as [bs eq]; rewrite eq;
   clear eq bs'.
 elim (veccase2 1 bs); intros b5 _H; destruct _H as [bs' eq]; rewrite eq;
   clear eq bs.
 elim (veccase2 0 bs'); intros b6 _H; destruct _H as [bs eq]; rewrite eq;
   clear eq bs'.
 rewrite (veccase1 bs).
 reflexivity.
Qed.

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

Definition decode (s: mlstring) : mlstring :=
  mlstring_of_asciilist (map ascii_of_bool8 (gcut 8 (cm (map tbl_inv
    (delete ascii_dec "="%char (asciilist_of_mlstring s))))(lt_O_Sn _))).

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
Proof.
 do 256(destruct n;auto);omega.
Qed.

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
  n <= 63 -> tblaux_inv (tblaux n) = n.
Proof.
 intros n H.
 unfold tblaux.
 simpl nat_of_ascii.
 destruct (n <=? 25).
  unfold tblaux_inv; simpl nat_of_ascii.
  elim (sumbool_rewrite_not (97 <=? 65 + n)); [ |omega].
  intros _H0 eq; rewrite eq.
  elim (sumbool_rewrite (65 <=? 65 + n)); [|apply le_plus_l].
  intros _H1 eq1; rewrite eq1.
  rewrite minus_plus; reflexivity.

  destruct (n <=? 51).
   unfold tblaux_inv; simpl nat_of_ascii.
   elim (sumbool_rewrite (97 <=? 97 + (n - 26))); [ |apply le_plus_l].
   intros _H0 eq0; rewrite eq0.
   rewrite (plus_comm 97 _).
   rewrite (plus_assoc).
   rewrite le_plus_minus_r;
     [|apply not_gt; intro; elim n0; apply (le_S_n _ _ H0)].
   rewrite plus_comm; rewrite minus_plus; reflexivity.
    
   destruct (n <=? 61).
    unfold tblaux_inv; simpl nat_of_ascii.
    elim (sumbool_rewrite_not (97 <=? 48 + (n-52)));
      [|simpl; omega].
    intros _H0 eq; rewrite eq.
    elim (sumbool_rewrite_not (65 <=? 48 + (n-52)));
      [ |clear n0 n1 l _H0 eq; omega].
    intros _H1 eq1; rewrite eq1.
    elim (sumbool_rewrite (48 <=? 48 + (n-52))); [|apply le_plus_l].
    intros _H2 eq2; rewrite eq2.
    rewrite (plus_comm 52 _); rewrite <- plus_assoc.
    rewrite minus_plus; rewrite plus_comm.
    rewrite le_plus_minus_r; [reflexivity |].
    apply not_gt; intro; elim n1.
    apply (le_S_n _ _ H0).
    
    destruct (n =? 62).
     rewrite e; reflexivity.

     unfold tblaux_inv; simpl.
     apply le_antisym; [|apply H].
     omega.
Qed.

Fact s_plus1 : forall n, S n = n + 1.
 Proof. intro n; rewrite plus_comm; reflexivity. Qed.

Lemma nat_of_bool6_le_63: forall bs: Vec bool 6,
  nat_of_ascii (ascii_of_bool6 bs) <= 63.
Proof.
 intros.
 elim (veccase2 5 bs); intros b1 _H; destruct _H as [bs' eq1]; rewrite eq1;
   clear eq1 bs.
 elim (veccase2 4 bs'); intros b2 _H; destruct _H as [bs eq]; rewrite eq;
   clear eq bs'.
 elim (veccase2 3 bs); intros b3 _H; destruct _H as [bs' eq]; rewrite eq;
   clear eq bs.
 elim (veccase2 2 bs'); intros b4 _H; destruct _H as [bs eq]; rewrite eq;
   clear eq bs'.
 elim (veccase2 1 bs); intros b5 _H; destruct _H as [bs' eq]; rewrite eq;
   clear eq bs.
 elim (veccase2 0 bs'); intros b6 _H; destruct _H as [bs eq]; rewrite eq;
   clear eq bs'.
 rewrite (veccase1 bs).
 unfold ascii_of_bool6.
 unfold nat_of_ascii.
 cut (forall b:bool, (if b then 1 else 0) <= 1); [intro Hb|intro b; case b; omega].
 (*B6*)
 rewrite (s_plus1 62).
 apply plus_le_compat; [|apply Hb].
 cut (62 = 2 * 31); [intro eq; rewrite eq; clear eq| reflexivity].
 apply (mult_le_compat_l).
 (*B5*)
 rewrite (s_plus1 30).
 apply plus_le_compat; [|apply Hb].
 cut (30 = 2 * 15); [intro eq; rewrite eq; clear eq| reflexivity].
 apply mult_le_compat_l.
 (*B4*)
 rewrite (s_plus1 14).
 apply plus_le_compat; [|apply Hb].
 cut (14 = 2 * 7); [intro eq; rewrite eq; clear eq| reflexivity].
 apply mult_le_compat_l. 
 (*B3*)
 rewrite (s_plus1 6).
 apply plus_le_compat; [|apply Hb].
 cut (6 = 2 * 3); [intro eq; rewrite eq; clear eq| reflexivity].
 apply mult_le_compat_l.
 (*B2*)
 rewrite (s_plus1 2).
 apply plus_le_compat; [|apply Hb].
 cut (2 = 2 * 1); [intro eq; rewrite eq; clear eq| reflexivity].
 apply mult_le_compat_l.
 (*B1*)
 case b1; omega.
Qed.

Lemma tbl_inv_inv: forall bs: Vec bool 6, tbl_inv (tbl bs) = bs.
Proof.
 unfold tbl.
 unfold tbl_inv.
 intro; rewrite ascii_nat_embedding_inv.
  rewrite tblaux_inv_inv.
   rewrite ascii_nat_embedding.
    apply ascii_bool6_inv.
    
    apply nat_of_bool6_le_63.

   apply tblaux_lt_256.
Qed.
 
Lemma digit_inv: forall c, ascii_of_bool8 (digit c) = c.
Proof.
 destruct c; reflexivity.
Qed.

Lemma tbl_not_eq : forall v, tbl v <> "="%char.
Proof.
 intro v; unfold tbl.
 unfold tblaux.
Admitted.

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