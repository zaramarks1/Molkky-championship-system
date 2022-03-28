package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Getter
@Entity
@Setter
@Table(name = "finnish")
public class Finnish {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "nbFinnish")
    private Integer nbFinnish;

    @Column(name = "nbTeamsQualified")
    private Integer nbTeamsQualified;

    @OneToMany(cascade = CascadeType.ALL)
    @JoinColumn(name = "idFinnish")
    private List<Match> matches = new ArrayList<>();

    @OneToOne(optional = false)
    @JoinColumn(name = "idRound")
    private Round round;

    public Finnish(){
    }

    public Finnish(Integer nbFinnish, Integer nbTeamsQualified){
        this.nbFinnish = nbFinnish;
        this.nbTeamsQualified = nbTeamsQualified;
    }
}
