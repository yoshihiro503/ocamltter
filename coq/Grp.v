Require Import List.
Require Import ListUtil.
Require Import Vec.

Section GRP.
  Require Import Arith.
(*  Infix "<=?" := le_lt_dec (at level 60).
  Infix "=?" := eq_nat_dec (at level 60).*)

  Definition make {A:Set} n (default:A) (xs: list A) : Vec A n.
    refine (
      fun A n default xs =>
        match split_at n xs as p return split_at n xs = p -> Vec A n with
          | (xs1, xs2) =>
            fun H => vec_of_list n (xs1 ++ replicate (n - length xs1) default) _
        end (refl_equal _)
    ).
    rewrite app_length.
    rewrite length_replicate.
    
    rewrite le_plus_minus_r; [reflexivity | ].
    apply (length_split_at n xs xs1 xs2).
    rewrite H; reflexivity.
  Defined.
  
  Require Import Recdef.
  Variable A : Set.
  Variable default : A.

  Definition res n : Set := ((Vec A n * list A) + {xs: list A | length xs < n})%type.
  Definition split_dec  (n:nat) (xs: list A) :
    (Vec A n * list A) + {xs : list A | length xs < n}.
   refine (fix split_dec n xs : res n :=
     match n as r return res r with
       | O   => inl _ (VNil, xs)
       | S m => 
         match xs as r return xs=r -> res (S m) with
           | nil   => fun eq => inr _ (exist _ xs _)
           | x::xs => fun eq =>
             match split_dec m xs with
               | inl (xs1, xs2)     => inl _ (VCons m x xs1, xs2)
               | inr (exist rest _) => inr _ (exist _ (x::rest) _)
             end
         end (refl_equal _)
     end
   ).
    rewrite eq; apply lt_O_Sn.
    
    simpl; apply lt_n_S; apply l.
  Defined.

  Lemma length_split_dec: forall n (xs tail: list A) (v : Vec A n),
    split_dec n xs = inl _ (v, tail) -> length tail <= length xs.
  Proof.
   induction n; intros.
    injection H; intros eq _; rewrite eq; apply le_refl.

    destruct xs; [discriminate H | ].
    generalize (IHn xs tail) H.
    simpl; case_eq (split_dec n xs).
     intros p; destruct p; intros.
     injection H2; intros eq eq2.
     apply le_S.
     apply (H1 v0); rewrite eq; reflexivity.
    
     intros.
     destruct s; discriminate H2.
   Qed.

  Lemma length_split_dec_lt: forall n (xs tail: list A) (v : Vec A n),
    0 < n -> split_dec n xs = inl _ (v, tail) -> length tail < length xs.
  Proof.
   intro n; case n; intros.
    elim (lt_n_O _ H).

    destruct xs; [discriminate H0 | ].
    generalize H0; simpl. 
    case_eq (split_dec n0 xs); intros; [ | destruct s; discriminate H2].

    apply le_lt_n_Sm.
    destruct p.
    apply (length_split_dec _ _ _ v0).
    injection H2.
    intros eq _; rewrite <- eq; rewrite H1; reflexivity.
  Qed.

  Lemma split_dec_inr : forall m,
    forall xs, length xs < m -> exists s, split_dec m xs = inr _ s.
  Proof.
   induction m; intros.
    elim (lt_n_O _ H).

    destruct xs.
     simpl.
     exists (exist _ nil _).
     reflexivity.

     simpl.
     elim (IHm xs); [ | apply (lt_S_n _ _ H)].
     intros.
     rewrite H0.
     destruct x.
     exists (exist _ (a::x) _).
     reflexivity.
  Qed.

  Function grp_aux (n:nat) (xs: list A) (H: 0 < n) {measure length xs}
    : list (Vec A n) * {rest: list A | length rest < n} :=
    match xs with
      | nil => (nil, exist _ nil H)
      | x::xs =>
        match split_dec n (x::xs) with
          | inl (v, tail) =>
            let (vs, rest) := grp_aux n tail H in
            (v::vs, rest)
          | inr r => (nil, r)
        end
    end.
   intros; apply (length_split_dec_lt _ _ _ v H teq0).
  Defined.

  Definition g1 n xs H := fst (grp_aux n xs H).

  Definition g2 n xs H :=
    match snd (grp_aux n xs H) with
      | exist ys HH => ys
    end.

  Lemma length_g2 : forall n xs H,
    length (g2 n xs H) < n.
  Proof.
   intros; unfold g2.
   destruct (grp_aux n xs H).
   destruct s.
   apply l0.
  Qed.

  Definition gcut := g1.
  Definition gfill (n:nat) (xs:list A) (H:0<n) : list (Vec A n).
    refine (fun n xs H =>
      let ys := g2 n xs H in
      (g1 n xs H) ++
      (vec_of_list n (ys ++ replicate(n-length ys) default) _ :: nil)).
    rewrite app_length.
    rewrite length_replicate.
    rewrite le_plus_minus_r; [reflexivity | ].
    apply lt_le_weak.
    apply (length_g2).
  Defined.

  Fixpoint cm {n:nat} (vs :list (Vec A n)) : list A :=
    match vs with
      | nil => nil
      | v::vs => list_of_vec v ++ cm vs
    end.

(*  Lemma gcut_cm1: forall m (vs:list (Vec A m)) xs H,
    gcut m (cm vs +++ xs) H = (vs +++ gcut m xs H).
  Proof.
   intros.
*)

(*  Lemma split_nil : forall m xs, length xs < m -> split_at m xs = (@Nil A, xs).
  Proof.
   induction m; intros.
    reflexivity.
    
    destruct xs.
     reflexivity.

     simpl.*)
  Lemma gcut_nil : forall m xs H, length xs < m -> gcut m xs H = nil.
  Proof.
   unfold gcut; unfold g1; intros.
   functional induction (grp_aux m xs H); try reflexivity.
   elim (split_dec_inr m (x::xs0)); [ | apply H0].
   intros x0 eq; rewrite eq in e0.
   discriminate e0.
  Qed.

  Lemma gcut_split : forall n xs H (v:Vec A n) tail,
    split_dec n xs = inl _ (v, tail) -> gcut n xs H = v :: gcut n tail H.
  Proof.
   unfold gcut; unfold g1; intros n xs H.
   functional induction (grp_aux n xs H); intros. (*不要だった*)
    destruct n; [elim (lt_n_O _ H) |].
    discriminate H0.

    rewrite e0 in H0.
    injection H0; intros.
    rewrite <- H1; rewrite e1.
    rewrite H2; reflexivity.
    
    rewrite e0 in H0; discriminate H0.
  Qed.

(*  Axiom gcut_split_inv : forall n xs H (v:Vec A n) tail,
    gcut n xs H = v :: gcut n tail H -> split_dec n xs = inl _ (v, tail).*)
  Axiom split_vec : forall m (v:Vec A m) ys,
    split_dec m (list_of_vec v ++ ys) = inl _ (v, ys).

  Lemma gcut_vec : forall m (v:Vec A m) xs H,
    gcut m (list_of_vec v ++ xs) H = v :: gcut m xs H.
  Proof.
   intros.
   rewrite (gcut_split _ (list_of_vec v ++ xs) _ v xs); [reflexivity | ].
   rewrite split_vec; reflexivity.
  Qed.

  Lemma gcut_cm: forall m (vs:list (Vec A m)) xs H,
    length xs < m -> gcut m (cm vs ++ xs) H = vs.
  Proof.
   induction vs; intros.
    apply (gcut_nil _ _ _ H0).

    simpl.
    rewrite app_ass.
    rewrite gcut_vec. 
    rewrite (IHvs _ H H0).
    reflexivity.
  Qed.

  Lemma cm_dist: forall n xs ys,
    @cm n (xs ++ ys) = (cm xs ++ cm ys).
  Proof.
   induction xs; intros; [reflexivity | ].
   simpl; rewrite app_ass; rewrite IHxs; reflexivity.
  Qed.

  Lemma ccc : forall xs, (xs ++ @nil A) = xs.
  Proof.
   induction xs; [|simpl; rewrite IHxs]; reflexivity.
  Qed.

    
  Axiom cm_g1_g2 : forall n xs H,
    (cm (g1 n xs H) ++ g2 n xs H) = xs.

(*いまここ*)
  Lemma grp_inv: forall n m H0 H1 (vs: list (Vec A m)),
    n < m -> gcut m (cm (gfill n (cm vs) H0)) H1 = vs.
  Proof.
   intros.
   unfold gfill.
   rewrite cm_dist.
   simpl; rewrite list_vec.
   rewrite ccc.
   rewrite <- app_ass.
   rewrite cm_g1_g2.
   rewrite gcut_cm; [reflexivity |].
   rewrite length_replicate.
   apply (le_lt_trans _ n).
    apply le_minus.
    
    apply H.
  Qed.


(*
            match (* length xs1 <= n *)
            match length xs1 =? n with
              | left HH =>
                let (vs, rest) := grp_aux n xs2 H in
                (vec_of_list_n n xs1 HH :: vs, rest)
              | right HH =>
                (Nil, exist _ xs1 HH)
Â            end
        end
    end.
   intros; apply (decrease_split_at n x xs0 xs1 _ H).
   rewrite teq0; reflexivity.
  Defined.
*)

(*
  Function grp_aux (n:nat) (xs: list A) (H: 0 < n) {measure length xs}
    : list (Vec A n) :=
    match xs with
      | Nil => Nil
      | x::xs =>
        let (xs1,xs2) := split_at n (x::xs) in
        make n default (x::xs) :: grp_aux n xs2 H
    end.
   intros; apply (decrease_split_at n x xs0 xs1 _ H).
   rewrite teq0; reflexivity.
  Defined.

  Definition grp n (xs : list A) : list (Vec A n) :=
    match n with
      | O => Nil
      | S m => grp_aux (S m) xs (lt_O_Sn m)
    end.
*)
End GRP.

Implicit Arguments gfill [A].
Implicit Arguments gcut [A].
Implicit Arguments grp_inv [A].
Implicit Arguments cm [A n].



