package com.molkky.molkky.domain;

import lombok.*;

import javax.persistence.*;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@Entity
@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "user_tournament_role")
public class UserTournamentRole implements Serializable {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "role")
    @Enumerated(EnumType.STRING)
    private type.UserRole role;

    @Column(name = "isRegistered")
    private Boolean isRegistered;

    @ManyToOne
    @JoinColumn(name="idTournament", nullable = true)
    private Tournament tournament;

    @ManyToOne
    @JoinColumn(name="idTeam", nullable = true)
    private Team team;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name="idUser", nullable = true)
    private User user;

    @OneToMany(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    @JoinColumn(name="idUserTournamentRole", nullable = true)
    private List<Notification> notifications = new ArrayList<>();
}
