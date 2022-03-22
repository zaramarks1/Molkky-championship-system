package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Getter
@Entity
@Setter
@Table(name = "court")
public class Court {
    @Id
    @GeneratedValue(strategy= GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "isAvailable")
    private boolean isAvailable;

    @Column(name = "name")
    private String name;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "court")
    private List<Match > matchs = new ArrayList<>();

    public Court(boolean isAvailable, String name) {
        this.isAvailable = isAvailable;
        this.name = name;
    }

    public Court() {

    }
}
