Require Import List.

Module Type QUEUE.
  Parameter Q : Type -> Type.
  
  Parameter empty  : forall A, Q A.
  Parameter inject : forall A, A -> Q A -> Q A.
  Parameter pop    : forall A, Q A -> Q A.
  Parameter peak   : forall A, Q A -> option A.

  Parameter to_list : forall A, Q A -> list A.

  Definition represents {A : Type} (q : Q A) (l : list A) : Prop :=
    to_list _ q = l.
  
  Axiom empty_is_empty : forall A,
    represents (empty A) nil.
  
  Axiom inject_spec :
    forall A l q x,
      represents q l ->
      represents (inject A x q) (l ++ (x :: nil)).
  
  Axiom pop_empty :
    forall A q,
      represents q nil ->
      represents (pop A q) nil.
  Axiom pop_xs :
    forall A q x xs,
      represents q (x::xs) ->
      represents (pop A q) xs.
  
  Axiom peak_empty_spec :
    forall A q,
      represents q nil ->
      peak A q = None.
  Axiom peak_spec :
    forall A q x xs,
      represents q (x::xs) ->
      peak A q = Some x.
End QUEUE.

Module BasicQueue <: QUEUE.

  Definition Q := list.

  Definition empty := @nil.

  Fixpoint inject {A : Type} (a : A) (q : Q A) : Q A :=
    match q with
      | nil => a :: nil
      | x :: q' => x :: inject a q'
    end.

  Lemma unfold_inject_base : 
    forall A (a : A),
      inject a nil = a :: nil.
  Proof. reflexivity. Qed.

  Lemma unfold_inject_step :
    forall A (a : A) x q,
      inject a (x :: q) = x :: inject a q.
  Proof. reflexivity. Qed.

  Definition pop {A : Type} (q : Q A) :=
    match q with
      | nil => nil
      | _ :: q' => q'
    end.
  Definition peak {A : Type} (q : Q A) :=
    match q with
      | nil => None
      | x :: _ => Some x
    end.
  Definition to_list {A : Type} (q : Q A) := q.
  
  Definition represents {A : Type} (q : Q A) (l : list A) : Prop :=
    to_list q = l.

  Lemma empty_is_empty : forall A,
    represents (empty A) nil.
  Proof.
    unfold represents.
    unfold to_list.
    unfold empty.
    reflexivity.
  Qed.

  Lemma inject_spec :
    forall A l q (x : A),
      represents q l ->
      represents (inject x q) (l ++ (x :: nil)).
  Proof.
    unfold represents, to_list.
    intros a l.
    induction l.
    intros.
    rewrite H.
    reflexivity.
    intros.
    rewrite H.
    rewrite unfold_inject_step.
    rewrite (IHl l x).
    rewrite app_comm_cons.
    reflexivity.
    reflexivity.
  Qed.

  Lemma pop_empty :
    forall A (q : Q A),
      represents q nil ->
      represents (pop q) nil.
  Proof.
    unfold represents, to_list.
    intros.
    rewrite H.
    reflexivity.
  Qed.

  Lemma pop_xs :
    forall A (q : Q A) x xs,
      represents q (x::xs) ->
      represents (pop q) xs.
  Proof.
    unfold represents, to_list.
    intros.
    rewrite H.
    unfold pop.
    reflexivity.
  Qed.
  
  Lemma peak_empty_spec :
    forall A (q : Q A),
      represents q nil ->
      peak q = None.
  Proof.
    unfold represents, to_list.
    intros.
    rewrite H.
    reflexivity.
  Qed.

  Lemma peak_spec :
    forall A (q : Q A) x xs,
      represents q (x::xs) ->
      peak q = Some x.
  Proof.
    unfold represents, to_list.
    intros.
    rewrite H.
    unfold peak.
    reflexivity.
  Qed.

End BasicQueue.

