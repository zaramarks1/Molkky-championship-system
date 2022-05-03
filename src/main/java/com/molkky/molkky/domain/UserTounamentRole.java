package com.molkky.molkky.domain;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import javax.persistence.*;

@Getter
@Entity
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "user_tounament_role")
public class UserTounamentRole {

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
    @JoinColumn(name="idTournement", nullable = true)
    private Tournament tournament;

    @ManyToOne
    @JoinColumn(name="idTeam", nullable = true)
    private Team team;

    @ManyToOne
    @JoinColumn(name="idUser", nullable = true)
    private User user;
}
