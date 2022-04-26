package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Getter
@Entity
@Setter
@Table(name = "pool")
public class Pool {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @OneToMany(cascade = CascadeType.ALL)
    @JoinColumn(name = "idPool")
    private List<Match> matches = new ArrayList<>();

    @OneToOne(optional = false)
    @JoinColumn(name = "idRound")
    private Round round;

    @Column(name = "finished")
    private boolean finished;

    public Pool(){
    }
}
