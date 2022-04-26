package com.molkky.molkky.domain.rounds;

import Type.RoundType;
import com.molkky.molkky.domain.Round;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;

@Getter
@Entity
@Setter
@DiscriminatorValue("Finnish")
public class Finnish extends Round {
    @Column(name = "nbFinnish")
    private Integer nbFinnish;

    @Column(name = "nbTeamsQualified")
    private Integer nbTeamsQualified;

    public Finnish(Integer nbFinnish, Integer nbTeams){
        super(RoundType.FINNISH, nbTeams);
        this.nbFinnish = nbFinnish;
    }

    public Finnish() {

    }
}
