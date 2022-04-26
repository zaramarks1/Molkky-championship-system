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
public class Finnish extends Round {
    @Column(name = "nbFinnish")
    private Integer nbFinnish;

    @Column(name = "nbTeamsQualified")
    private Integer nbTeamsQualified;


    public Finnish(){
    }

    public Finnish(Integer nbFinnish, Integer nbTeamsQualified){
        this.nbFinnish = nbFinnish;
        this.nbTeamsQualified = nbTeamsQualified;
    }
}
