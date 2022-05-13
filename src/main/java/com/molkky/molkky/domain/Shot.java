package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.io.Serializable;

@Getter
@Entity
@Setter
@Table(name = "shot")
public class Shot implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "score")
    private Integer score;

    @ManyToOne(optional = false)
    @JoinColumn(name = "match_id", nullable = false)
    private Set set;

    @ManyToOne(optional = false)
    @JoinColumn(name = "team_id", nullable = false)
    private Team team;

    public Team getTeam() {
        return team;
    }
}