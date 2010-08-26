Require Import List.

Inductive Vec (A: Type): nat -> Type :=
| VNil : Vec A O
| VCons : forall n, A -> Vec A n -> Vec A (S n).

Implicit Arguments VNil [A].
Implicit Arguments VCons [A].

Definition vec_of_list {A:Type} n (xs:list A): length xs = n -> Vec A n.
 refine (
   fix iter A n xs {struct xs} : length xs = n -> Vec A n :=
     match xs with
       | nil => _
       | x::xs => _
     end
 ).
 intro H; rewrite <- H; exact VNil.

 simpl.
 intro H; rewrite <- H; exact (VCons _ x (iter A (length xs0) xs0 (refl_equal _))).
Defined.

Fixpoint list_of_vec {A:Type} {n:nat} (v:Vec A n) : list A :=
  match v with
    | VNil => nil
    | VCons _ x xs => x :: list_of_vec xs
  end.

Lemma list_vec : forall {A:Type} (xs:list A) n (H: length xs = n),
  list_of_vec (vec_of_list n xs H) = xs.
Proof.
 induction xs; intros; case H; simpl; [ | rewrite IHxs]; reflexivity.
Qed.
