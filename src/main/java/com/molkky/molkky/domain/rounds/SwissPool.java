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
public class SwissPool extends Round{
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
