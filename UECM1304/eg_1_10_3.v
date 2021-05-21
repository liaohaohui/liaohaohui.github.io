Example eg1_10_3: forall (X : Type)
  (P Q R : X -> Prop) (c : X),
  ~ R c
  -> (forall t : X, P t -> Q t)
  -> (forall t : X, Q t -> R t)
  -> ~ P c.
Proof.
 intros X P Q R c.
 intros H1 H2 H3.
 unfold not in H1.
 specialize (H3 c).
 specialize (H2 c).
 intro H4.
 apply H2 in H4.
 apply H3 in H4.
 apply H1 in H4.
 assumption.
Qed.