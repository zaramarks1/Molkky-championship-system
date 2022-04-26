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
@DiscriminatorValue("Knockout")
public class Knockout extends Round{
    @Column(name = "nbSets")
    private Integer nbSets;

    public Knockout(){
        this.setType(RoundType.KNOCKOUT);
    }

    public Knockout(Integer nbSets){
        this.nbSets = nbSets;
    }
}
