package com.molkky.molkky.domain.rounds;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Round;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Getter
@Entity
@Setter
public class Knockout extends Round{
    @Column(name = "nbSets")
    private Integer nbSets;

    public Knockout(){
    }

    public Knockout(Integer nbSets){
        this.nbSets = nbSets;
    }
}
