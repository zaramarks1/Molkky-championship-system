package com.molkky.molkky.domain.rounds;

import type.RoundType;
import com.molkky.molkky.domain.Round;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;

@Getter
@Entity
@Setter
@DiscriminatorValue("SwissPool")
public class SwissPool extends Round{
    @Column(name = "nbSubRounds")
    private Integer nbSubRounds;

    @Column(name = "nbTeamsQualified")
    private Integer nbTeamsQualified;

    public SwissPool(){
        this.setType(RoundType.SWISSPOOL);
    }

    public SwissPool(Integer nbSubRounds, Integer nbTeamsQualified){
        this.nbSubRounds = nbSubRounds;
        this.setType(RoundType.SWISSPOOL);
        this.nbTeamsQualified = nbTeamsQualified;
    }
}
