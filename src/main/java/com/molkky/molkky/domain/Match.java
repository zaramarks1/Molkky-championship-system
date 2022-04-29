package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;
import java.util.List;

@Getter
@Entity
@Setter
@Table(name = "molkky_match")
@NoArgsConstructor
public class Match {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "finished")
    private Boolean finished = false;

    @Column(name = "nbSets")
    private Integer nbSets;

    @OneToOne(optional = true)
    private Team winner;

    @ManyToMany
    @JoinTable(name = "molkky_match_team",
            joinColumns = @JoinColumn(name = "match_id"),
            inverseJoinColumns = @JoinColumn(name = "team_id"))
    private List<Team> teams;

    @OneToMany(mappedBy = "match", cascade = CascadeType.ALL)
    private List<Set> sets;
}
