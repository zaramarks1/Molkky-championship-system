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
    @GeneratedValue(strategy= GenerationType.AUTO)
    @Column(name = "id")
    private Integer id;

    @Column(name = "isAvailable")
    private boolean isAvailable;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "court")
    private List<Match > matchs = new ArrayList<>();

    public Court(Integer id, boolean isAvailable) {
        this.id = id;
        this.isAvailable = isAvailable;
    }

    public Court() {

    }
}
