package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Getter
@Entity
@Setter
@Table(name = "simplegame")
public class SimpleGame {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "nbSets")
    private Integer nbSets;

    @OneToMany(cascade = CascadeType.ALL)
    @JoinColumn(name = "idSimplegame")
    private List<Match> matches = new ArrayList<>();

    public SimpleGame(){
    }
}
