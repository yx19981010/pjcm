package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.Material;
import com.samsph.pjcm.model.PostNoOffset;
import org.springframework.data.jpa.repository.JpaRepository;

public interface PostNoOffsetRepository extends JpaRepository<PostNoOffset, Integer> {
}
