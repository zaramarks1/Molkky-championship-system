package com.molkky.molkky.domain.rounds;

import com.molkky.molkky.domain.Phase;
import type.PhaseType;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.print.attribute.standard.Finishings;

@Getter
@Entity
@Setter
@DiscriminatorValue("FINNISH")
public class Finnish extends Phase {

    @Column(name = "nbFinnish")
    private Integer nbFinnish;

    @Column(name = "nbTeamsQualified")
    private Integer nbTeamsQualified;


    public Finnish(){
    }

}
