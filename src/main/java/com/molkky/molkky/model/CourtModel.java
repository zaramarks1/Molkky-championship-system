package com.molkky.molkky.model;

import com.molkky.molkky.domain.Court;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class CourtModel {
    private Integer id;
    private boolean isAvailable;
    private String name;

    public CourtModel(Court court) {
        this.id = court.getId();
        this.isAvailable = court.isAvailable();
        this.name = court.getName();
    }
}
