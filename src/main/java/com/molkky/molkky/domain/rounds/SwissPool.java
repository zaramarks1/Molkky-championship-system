package com.molkky.molkky.domain.rounds;

import com.molkky.molkky.domain.Phase;
import type.PhaseType;
import com.molkky.molkky.domain.Round;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;

@Getter
@Entity
@Setter
@DiscriminatorValue("SWISSPOOL")
public class SwissPool extends Phase {
    @Column(name = "nbSubRounds")
    private Integer nbSubRounds;

    @Column(name = "nbTeamsQualified")
    private Integer nbTeamsQualified;

    public SwissPool(){

    }

    public SwissPool(Integer nbSubRounds, Integer nbTeamsQualified){
        this.nbSubRounds = nbSubRounds;
        this.nbTeamsQualified = nbTeamsQualified;
    }
}
