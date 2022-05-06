package com.molkky.molkky.domain.rounds;

import com.molkky.molkky.domain.Phase;
import lombok.Data;
import type.PhaseType;
import com.molkky.molkky.domain.Round;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;


@Entity
@Data
@DiscriminatorValue("KNOCKOUT")
public class Knockout extends Phase {

    @Column(name = "nbMatch")
    private Integer nbMatch;

    /*quarter-finals height of finals etc*/
    @Column(name = "teamsRemaining")
    private Integer teamsRemaining;


}
