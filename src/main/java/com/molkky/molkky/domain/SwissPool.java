package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Getter
@Entity
@Setter
@Table(name = "swisspool")
public class SwissPool {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "nbSubRounds")
    private Integer nbSubRounds;

    @Column(name = "nbTeamsQualified")
    private Integer nbTeamsQualified;

    @OneToMany(cascade = CascadeType.ALL)
    @JoinColumn(name = "idSwisspool")
    private List<Match> matches = new ArrayList<>();

    @OneToOne(optional = false)
    @JoinColumn(name = "idRound")
    private Round round;

    @Column(name = "finished")
    private Boolean finished = false;

    public SwissPool(){
    }

    public SwissPool(Integer nbSubRounds, Integer nbTeamsQualified){
        this.nbSubRounds = nbSubRounds;
        this.nbTeamsQualified = nbTeamsQualified;
    }
}
