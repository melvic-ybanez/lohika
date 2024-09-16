package com.melvic.lohika

import com.melvic.lohika.formula.Formula

final case class Entailment(premises: List[Formula], conclusion: Formula)